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

unit dxRichEdit.DocumentModel.Section;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxGenerics;

type
  TdxSection = class;

  TdxSectionHeadersFootersBase = class;

  { TdxSectionHeaderFooterBase }

  TdxSectionHeaderFooterBase = class abstract(TdxContentTypeBase)
  strict private
    FType: TdxHeaderFooterType;
    FOnRequestSectionIndex: TdxRequestSectionIndexEventHandler;
  protected
    function GetIsMain: Boolean; override;
    function GetIsHeaderFooter: Boolean; override;
    function RaiseRequestSectionIndex: TdxSectionIndex; virtual;
    function CalculateMainPieceTableStartRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    function CalculateMainPieceTableEndRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex; virtual;
    procedure OverrideType(AType: TdxHeaderFooterType); virtual;
    function GetCaption: string; virtual; abstract;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; AType: TdxHeaderFooterType);
    procedure ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); override;
    procedure ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex); override;
    procedure ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase); override;
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); override;
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; override;
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; virtual; abstract;
    function GetSection: TdxSection; virtual;

    // for internal use
    function GetSectionIndex: TdxSectionIndex; virtual;

    property Caption: string read GetCaption;
    property &Type: TdxHeaderFooterType read FType;
    property RequestSectionIndex: TdxRequestSectionIndexEventHandler read FOnRequestSectionIndex;
  end;

  { TdxHeaderFooterCollectionBase }

  TdxHeaderFooterCollectionBase<T: TdxContentTypeBase; U> = class abstract(TdxObjectList<T>)
  public
    procedure Clear; override;
  end;

  { TdxSectionHeadersFootersBase }

  TdxSectionHeadersFootersBase = class abstract
  strict private
    FSection: TdxSection;
    function GetDocumentModel: TdxCustomDocumentModel;
  protected
    property Section: TdxSection read FSection;
    property DocumentModel: TdxCustomDocumentModel read GetDocumentModel;
  public
    constructor Create(ASection: TdxSection); overload;
    procedure Add(AType: TdxHeaderFooterType); overload; virtual; abstract;
    procedure Remove(AType: TdxHeaderFooterType); virtual; abstract;
    function CanLinkToPrevious(AType: TdxHeaderFooterType): Boolean; virtual; abstract;
    function IsLinkedToPrevious(AType: TdxHeaderFooterType): Boolean; virtual; abstract;
    function IsLinkedToNext(AType: TdxHeaderFooterType): Boolean; virtual; abstract;
    procedure LinkToPrevious(AType: TdxHeaderFooterType); virtual; abstract;
    procedure LinkToNext(AType: TdxHeaderFooterType); virtual; abstract;
    function ShouldRelinkNextSection(AType: TdxHeaderFooterType): Boolean; virtual; abstract;
    function ShouldRelinkPreviousSection(AType: TdxHeaderFooterType): Boolean; virtual; abstract;
    procedure UnlinkFromPrevious(AType: TdxHeaderFooterType); virtual; abstract;
    procedure UnlinkFromNext(AType: TdxHeaderFooterType); virtual; abstract;
    function GetObjectCore(AType: TdxHeaderFooterType): TdxSectionHeaderFooterBase; virtual; abstract;
    function CalculateActualObjectCore(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TdxSectionHeaderFooterBase;
    function CalculateActualObjectType(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TdxHeaderFooterType;
  end;

  { TdxSectionHeaderFooterIndexChangedHistoryItem }

  TdxSectionHeaderFooterIndexChangedHistoryItem<T> = class abstract(TdxHistoryItem)
  strict private
    FSectionIndex: TdxSectionIndex;
    FType: TdxHeaderFooterType;
    FPreviousIndex: T;
    FNextIndex: T;
    class function GetMainPieceTable(ASection: TdxSection): TdxCustomPieceTable; static;
  protected
    procedure RedoCore; override;
    procedure UndoCore; override;
    procedure ReplaceHeaderFooter(AIndex: T); virtual;
    procedure ApplyChanges(ASection: TdxSection); virtual;
    procedure SetCurrentHeaderFooterIndex(ASection: TdxSection; AIndex: T); virtual; abstract;
  public
    constructor Create(ASection: TdxSection); reintroduce;

    property &Type: TdxHeaderFooterType read FType write FType;
    property PreviousIndex: T read FPreviousIndex write FPreviousIndex;
    property NextIndex: T read FNextIndex write FNextIndex;
  end;

  { TdxSectionPageHeaderIndexChangedHistoryItem }

  TdxSectionPageHeaderIndexChangedHistoryItem = class(TdxSectionHeaderFooterIndexChangedHistoryItem<TdxHeaderIndex>)
  protected
    procedure SetCurrentHeaderFooterIndex(ASection: TdxSection; AIndex: TdxHeaderIndex); override;
  end;

  { TdxSectionHeadersFooters }

  TdxSectionHeadersFooters<TObj: TdxSectionHeaderFooterBase> = class abstract(TdxSectionHeadersFootersBase)
  public type
    TdxLinkObjectDelegate = reference to procedure(ATargetObject: TdxSectionHeadersFooters<TObj>;
      AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex);
  strict private
    FIndices: TArray<TdxSectionIndex>;
  protected
    function GetInvalidIndex: TdxSectionIndex; virtual; abstract;
    function GetObjectCache: TdxHeaderFooterCollectionBase<TObj, TdxSectionIndex>; virtual; abstract;
    function CreateEmptyObjectCore(AType: TdxHeaderFooterType): TObj; virtual; abstract;
    function CreateIndex(AValue: Integer): TdxSectionIndex; virtual; abstract;
    function CreateHistoryItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxSectionIndex>; virtual; abstract;
    function GetObjectProvider(ASection: TdxSection): TdxSectionHeadersFooters<TObj>; virtual; abstract;
    procedure SubscribeEvents; virtual;
    procedure UnsubscribeEvents; virtual;
    procedure SubscribeObjectEvents(AObj: TObj); virtual;
    procedure UnsubscribeObjectEvents(AObj: TObj); virtual;
    procedure OnRequestSectionIndex(ASender: TObject; AArgs: TdxRequestSectionIndexEventArgs);
    function GetValidUniqueObjectIndices: TdxIntegerList; virtual;
    procedure PerformLinkToPrevious(AType: TdxHeaderFooterType; ALink: Boolean; const ALinkAction: TdxLinkObjectDelegate); virtual;
    procedure PerformLinkToNext(AType: TdxHeaderFooterType; ALink: Boolean; const ALinkAction: TdxLinkObjectDelegate); virtual;
    class procedure LinkCore(ATargetObject: TdxSectionHeadersFooters<TObj>; AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex); static;
    class procedure UnlinkCore(ATargetObject: TdxSectionHeadersFooters<TObj>; AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex); overload; static;
    procedure UnlinkCore(AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex); overload;
    function GetObjectIndex(AType: TdxHeaderFooterType): TdxSectionIndex;
    procedure SetObjectIndex(AType: TdxHeaderFooterType; AIndex: TdxSectionIndex);
    function CreateEmptyObject(AType: TdxHeaderFooterType): TdxSectionIndex;
    function CreateObjectDeepCopy(ASourceObject: TObj): TdxSectionIndex;
    procedure CopyObjectContent(ASourceHeaderFooter: TObj; ATargetHeaderFooter: TObj);
    procedure ChangeObjectIndex(AType: TdxHeaderFooterType; ANewIndex: TdxSectionIndex); virtual;

    property InvalidIndex: TdxSectionIndex read GetInvalidIndex;
    property ObjectCache: TdxHeaderFooterCollectionBase<TObj, TdxSectionIndex> read GetObjectCache;
  public
    constructor Create(ASection: TdxSection); overload;

    procedure Add(AType: TdxHeaderFooterType); override;
    procedure CopyFrom(ASource: TdxSectionHeadersFooters<TObj>); overload; virtual;
    procedure CopyFrom(ASourceSection: TdxSection); overload; virtual;
    procedure Remove(AType: TdxHeaderFooterType); override;
    function CanLinkToPrevious(AType: TdxHeaderFooterType): Boolean; override;
    function GetObjectCore(AType: TdxHeaderFooterType): TdxSectionHeaderFooterBase; override;
    function IsLinkedToPrevious(AType: TdxHeaderFooterType): Boolean; override;
    function IsLinkedToNext(AType: TdxHeaderFooterType): Boolean; override;
    procedure LinkToPrevious(AType: TdxHeaderFooterType); override;
    procedure UnlinkFromPrevious(AType: TdxHeaderFooterType); override;
    procedure LinkToNext(AType: TdxHeaderFooterType); override;
    procedure UnlinkFromNext(AType: TdxHeaderFooterType); override;
    function CalculateActualObject(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TObj;
    function ShouldRelinkNextSection(AType: TdxHeaderFooterType): Boolean; override;
    function ShouldRelinkPreviousSection(AType: TdxHeaderFooterType): Boolean; override;
    function GetObject(AType: TdxHeaderFooterType): TObj;
  end;

  { TdxSectionHeader }

  TdxSectionHeader = class(TdxSectionHeaderFooterBase)
  protected
    function GetIsHeader: Boolean; override;
    function GetCaption: string; override;
  public
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  end;

  { TdxHeaderCollection }

  TdxHeaderCollection = class(TdxHeaderFooterCollectionBase<TdxSectionHeader, TdxHeaderIndex>);

  { TdxSectionHeaders }

  TdxSectionHeaders = class(TdxSectionHeadersFooters<TdxSectionHeader>)
  protected
    function GetInvalidIndex: TdxHeaderIndex; override;
    function GetObjectCache: TdxHeaderFooterCollectionBase<TdxSectionHeader, TdxHeaderIndex>; override;
    function CreateEmptyObjectCore(AType: TdxHeaderFooterType): TdxSectionHeader; override;
    function CreateIndex(AValue: Integer): TdxHeaderIndex; override;
    function CreateHistoryItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxHeaderIndex>; override;
    function GetObjectProvider(ASection: TdxSection): TdxSectionHeadersFooters<TdxSectionHeader>; override;
  end;

  { TdxSectionFooter }

  TdxSectionFooter = class(TdxSectionHeaderFooterBase)
  protected
    function GetIsFooter: Boolean; override;
    function GetCaption: string; override;
  public
    function GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase; override;
  end;

  { TdxFooterCollection }

  TdxFooterCollection = class(TdxHeaderFooterCollectionBase<TdxSectionFooter, TdxFooterIndex>);

  { TdxSectionPageFooterIndexChangedHistoryItem }

  TdxSectionPageFooterIndexChangedHistoryItem = class(TdxSectionHeaderFooterIndexChangedHistoryItem<TdxFooterIndex>)
  protected
    procedure SetCurrentHeaderFooterIndex(ASection: TdxSection; AIndex: TdxFooterIndex); override;
  end;

  { TdxSectionFooters }

  TdxSectionFooters = class(TdxSectionHeadersFooters<TdxSectionFooter>)
  protected
    function GetInvalidIndex: TdxFooterIndex; override;
    function GetObjectCache: TdxHeaderFooterCollectionBase<TdxSectionFooter, TdxFooterIndex>; override;
    function CreateEmptyObjectCore(AType: TdxHeaderFooterType): TdxSectionFooter; override;
    function CreateIndex(AValue: Integer): TdxFooterIndex; override;
    function CreateHistoryItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxFooterIndex>; override;
    function GetObjectProvider(ASection: TdxSection): TdxSectionHeadersFooters<TdxSectionFooter>; override;
  end;

  { TdxSection }

  TdxSection = class(TdxSimpleSection)
  strict private
    FMargins: TdxSectionMargins;
    FColumns: TdxSectionColumns;
    FPage: TdxSectionPage;
    FGeneralSettings: TdxSectionGeneralSettings;
    FPageNumbering: TdxSectionPageNumbering;
    FLineNumbering: TdxSectionLineNumbering;
    FFootNote: TdxSectionFootNote;
    FEndNote: TdxSectionFootNote;
    FHeaders: TdxSectionHeaders;
    FFooters: TdxSectionFooters;
    function GetHasNonEmptyHeadersOrFooters: Boolean;
    function GetFirstPageHeader: TdxSectionHeader;
    function GetInnerFirstPageHeader: TdxSectionHeader;
    function GetInnerFirstPageHeaderIndex: TdxHeaderIndex;
    procedure SetInnerFirstPageHeaderIndex(const AValue: TdxHeaderIndex);
    function GetInnerOddPageHeader: TdxSectionHeader;
    function GetInnerOddPageHeaderIndex: TdxHeaderIndex;
    procedure SetInnerOddPageHeaderIndex(const AValue: TdxHeaderIndex);
    function GetEvenPageHeader: TdxSectionHeader;
    function GetInnerEvenPageHeader: TdxSectionHeader;
    function GetInnerEvenPageHeaderIndex: TdxHeaderIndex;
    procedure SetInnerEvenPageHeaderIndex(const AValue: TdxHeaderIndex);
    function GetFirstPageFooter: TdxSectionFooter;
    function GetInnerFirstPageFooter: TdxSectionFooter;
    function GetInnerFirstPageFooterIndex: TdxFooterIndex;
    procedure SetInnerFirstPageFooterIndex(const AValue: TdxFooterIndex);
    function GetInnerOddPageFooter: TdxSectionFooter;
    function GetInnerOddPageFooterIndex: TdxFooterIndex;
    procedure SetInnerOddPageFooterIndex(const AValue: TdxFooterIndex);
    function GetEvenPageFooter: TdxSectionFooter;
    function GetInnerEvenPageFooter: TdxSectionFooter;
    function GetInnerEvenPageFooterIndex: TdxFooterIndex;
    procedure SetInnerEvenPageFooterIndex(const AValue: TdxFooterIndex);
  protected
    class function GetMainPieceTable(ADocumentModel: TdxCustomDocumentModel): TdxCustomPieceTable;{TdxPieceTable; }static;

    procedure OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs); virtual;
    function GetPreviousSection: TdxSection; virtual;
    function GetNextSection: TdxSection; virtual;
    function GetPreviousSectionHeader(AHeader: TdxSectionFooter): TdxSectionHeader; virtual;
    function GetPreviousSectionFooter(AFooter: TdxSectionFooter): TdxSectionFooter; virtual;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;
    procedure AddPieceTables(AResult: TdxFastList; AIncludeUnreferenced: Boolean);
    procedure CopyFrom(ASection: TdxCustomSection); override;
    procedure CopyFromCore(const ASection: TdxSection); virtual;
    function Copy(CopyManager: TObject): TdxSection; virtual;
    function GetActualColumnsCount: Integer;
    function GetCorrespondingHeader(AFooter: TdxSectionFooter): TdxSectionHeader; virtual;
    function GetCorrespondingFooter(AHeader: TdxSectionHeader): TdxSectionFooter; virtual;
    procedure Reset;
    function IsHidden: Boolean; virtual;
    procedure SubscribeInnerObjectsEvents; override;
    procedure UnsubscribeInnerObjectsEvents; override;
    procedure SubscribeHeadersFootersEvents; override;
    procedure UnsubscribeHeadersFootersEvents; override;

    property Margins: TdxSectionMargins read FMargins;
    property Columns: TdxSectionColumns read FColumns;
    property Page: TdxSectionPage read FPage;
    property GeneralSettings: TdxSectionGeneralSettings read FGeneralSettings;
    property PageNumbering: TdxSectionPageNumbering read FPageNumbering;
    property LineNumbering: TdxSectionLineNumbering read FLineNumbering;
    property FootNote: TdxSectionFootNote read FFootNote;
    property EndNote: TdxSectionFootNote read FEndNote;
    property HasNonEmptyHeadersOrFooters: Boolean read GetHasNonEmptyHeadersOrFooters;
    property Headers: TdxSectionHeaders read FHeaders;
    property Footers: TdxSectionFooters read FFooters;
    property FirstPageHeader: TdxSectionHeader read GetFirstPageHeader;
    property InnerFirstPageHeader: TdxSectionHeader read GetInnerFirstPageHeader;
    property InnerFirstPageHeaderIndex: TdxHeaderIndex read GetInnerFirstPageHeaderIndex write SetInnerFirstPageHeaderIndex;
    property OddPageHeader: TdxSectionHeader read GetInnerOddPageHeader;
    property InnerOddPageHeader: TdxSectionHeader read GetInnerOddPageHeader;
    property InnerOddPageHeaderIndex: TdxHeaderIndex read GetInnerOddPageHeaderIndex write SetInnerOddPageHeaderIndex;
    property EvenPageHeader: TdxSectionHeader read GetEvenPageHeader;
    property InnerEvenPageHeader: TdxSectionHeader read GetInnerEvenPageHeader;
    property InnerEvenPageHeaderIndex: TdxHeaderIndex read GetInnerEvenPageHeaderIndex write SetInnerEvenPageHeaderIndex;
    property FirstPageFooter: TdxSectionFooter read GetFirstPageFooter;
    property InnerFirstPageFooter: TdxSectionFooter read GetInnerFirstPageFooter;
    property InnerFirstPageFooterIndex: TdxFooterIndex read GetInnerFirstPageFooterIndex write SetInnerFirstPageFooterIndex;
    property OddPageFooter: TdxSectionFooter read GetInnerOddPageFooter;
    property InnerOddPageFooter: TdxSectionFooter read GetInnerOddPageFooter;
    property InnerOddPageFooterIndex: TdxFooterIndex read GetInnerOddPageFooterIndex write SetInnerOddPageFooterIndex;
    property EvenPageFooter: TdxSectionFooter read GetEvenPageFooter;
    property InnerEvenPageFooter: TdxSectionFooter read GetInnerEvenPageFooter;
    property InnerEvenPageFooterIndex: TdxFooterIndex read GetInnerEvenPageFooterIndex write SetInnerEvenPageFooterIndex;
    property Previous: TdxSection read GetPreviousSection;
    property Next: TdxSection read GetNextSection;
  end;

  TdxSectionCollection = class(TdxSimpleSectionCollection)
  private
    function GetItem(Index: Integer): TdxSection;
    procedure SetItem(Index: Integer; const Value: TdxSection);
  public
    function First: TdxSection; reintroduce;
    function Last: TdxSection; reintroduce;

    property Items[Index: Integer]: TdxSection read GetItem write SetItem; default;
  end;

  { TdxSectionEventArgs }

  TdxSectionEventArgs = class(TdxEventArgs)
  strict private
    FSectionIndex: TdxSectionIndex;
  public
    constructor Create(ASectionIndex: TdxSectionIndex);
    property SectionIndex: TdxSectionIndex read FSectionIndex;
  end;

  TdxSectionEvent = procedure(Sender: TObject; E: TdxSectionEventArgs) of object;
  TdxSectionEventHandler = TdxMulticastMethod<TdxSectionEvent>;

  { TdxSectionProperties }

  TdxSectionProperties = class
  strict private
    FPageWidth: Integer;
    FPageHeight: Integer;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FBottomMargin: Integer;
    FColumnInfoCollection: TdxColumnInfoCollection;
    FEqualWidthColumns: Boolean;
    FColumnCount: Integer;
    FSection: TdxSection;
    FSpace: Integer;
  public
    constructor Create(ASection: TdxSection); overload;
    constructor Create(ASectionProperties: TdxSectionProperties); overload;
    destructor Destroy; override;
    procedure CopyToSection(ASection: TdxSection);

    property Section: TdxSection read FSection;
    property ColumnInfoCollection: TdxColumnInfoCollection read FColumnInfoCollection;
    property EqualWidthColumns: Boolean read FEqualWidthColumns write FEqualWidthColumns;
    property ColumnCount: Integer read FColumnCount;
    property Space: Integer read FSpace write FSpace;
    property PageWidth: Integer read FPageWidth write FPageWidth;
    property PageHeight: Integer read FPageHeight write FPageHeight;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    property RightMargin: Integer read FRightMargin write FRightMargin;
    property TopMargin: Integer read FTopMargin write FTopMargin;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
  end;

implementation

uses
  RTLConsts, Contnrs,
  cxGeometry, dxCore, cxClasses, dxHash, dxHashUtils,

  dxRichEdit.Strs,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.Utils.Exceptions, Math;

{ TdxSection }

constructor TdxSection.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FMargins := TdxSectionMargins.Create(ADocumentModel);
  FColumns := TdxSectionColumns.Create(ADocumentModel);
  FPage := TdxSectionPage.Create(ADocumentModel);
  FGeneralSettings := TdxSectionGeneralSettings.Create(ADocumentModel);
  FPageNumbering := TdxSectionPageNumbering.Create(ADocumentModel);
  FLineNumbering := TdxSectionLineNumbering.Create(ADocumentModel);
  FFootNote := TdxSectionFootNote.Create(ADocumentModel);
  FEndNote := TdxSectionFootNote.Create(ADocumentModel);
  FEndNote.SetIndexInitial(TdxFootNoteInfoCache.DefaultEndNoteItemIndex);

  FHeaders := TdxSectionHeaders.Create(Self);
  FFooters := TdxSectionFooters.Create(Self);

  SubscribeInnerObjectsEvents;
end;

destructor TdxSection.Destroy;
begin
  FreeAndNil(FFooters);
  FreeAndNil(FHeaders);
  FreeAndNil(FFootNote);
  FreeAndNil(FEndNote);
  FreeAndNil(FLineNumbering);
  FreeAndNil(FPageNumbering);
  FreeAndNil(FGeneralSettings);
  FreeAndNil(FColumns);
  FreeAndNil(FMargins);
  FreeAndNil(FPage);
  inherited Destroy;
end;

procedure TdxSection.CopyFrom(ASection: TdxCustomSection);
var
  ASource: TdxSection;
begin
  Assert(ASection.DocumentModel = DocumentModel);
  ASource := Safe<TdxSection>.Cast(ASection);
  Assert(ASource <> nil);
  CopyFromCore(ASource);

  Headers.CopyFrom(ASource.Headers);
  Footers.CopyFrom(ASource.Footers);
  inherited CopyFrom(ASection);
end;

function TdxSection.GetHasNonEmptyHeadersOrFooters: Boolean;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
begin
  AHeaderFooter := InnerFirstPageHeader;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);
  AHeaderFooter := InnerOddPageHeader;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);
  AHeaderFooter := InnerEvenPageHeader;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);
  AHeaderFooter := InnerFirstPageFooter;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);
  AHeaderFooter := InnerOddPageFooter;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);
  AHeaderFooter := InnerEvenPageFooter;
  if (AHeaderFooter <> nil) and not AHeaderFooter.PieceTable.IsEmpty then
    Exit(True);

  Result := False;
end;

procedure TdxSection.SubscribeInnerObjectsEvents;
begin
  FMargins.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FColumns.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FPage.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FGeneralSettings.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FPageNumbering.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FLineNumbering.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FFootNote.OnObtainAffectedRange.Add(OnObtainAffectedRange);
  FEndNote.OnObtainAffectedRange.Add(OnObtainAffectedRange);
end;

procedure TdxSection.CopyFromCore(const ASection: TdxSection);
begin
  Margins.CopyFrom(ASection.Margins.Info);
  Columns.CopyFrom(ASection.Columns.Info);
  Page.CopyFrom(ASection.Page.Info);
  GeneralSettings.CopyFrom(ASection.GeneralSettings.Info);
  PageNumbering.CopyFrom(ASection.PageNumbering.Info);
  LineNumbering.CopyFrom(ASection.LineNumbering.Info);
  FootNote.CopyFrom(ASection.FootNote.Info);
  EndNote.CopyFrom(ASection.EndNote.Info);
end;

procedure TdxSection.OnObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
var
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphs := TdxDocumentModel(DocumentModel).MainPieceTable.Paragraphs;
  if FirstParagraphIndex >= 0 then
  begin
    E.Start := AParagraphs[FirstParagraphIndex].FirstRunIndex;
    E.&End := AParagraphs[LastParagraphIndex].LastRunIndex;
  end;
end;

procedure TdxSection.AddPieceTables(AResult: TdxFastList{<TdxPieceTable>}; AIncludeUnreferenced: Boolean);
begin
  if InnerFirstPageHeader <> nil then
    InnerFirstPageHeader.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
  if InnerOddPageHeader <> nil then
    InnerOddPageHeader.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
  if InnerEvenPageHeader <> nil then
    InnerEvenPageHeader.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);

  if InnerFirstPageFooter <> nil then
    InnerFirstPageFooter.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
  if InnerOddPageFooter <> nil then
    InnerOddPageFooter.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
  if InnerEvenPageFooter <> nil then
    InnerEvenPageFooter.PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
end;

procedure TdxSection.SubscribeHeadersFootersEvents;
begin
  Headers.SubscribeEvents;
  Footers.SubscribeEvents;
end;

class function TdxSection.GetMainPieceTable(ADocumentModel: TdxCustomDocumentModel): TdxCustomPieceTable;
begin
  Assert(ADocumentModel <> nil);
  Result := ADocumentModel.MainPart;
end;

procedure TdxSection.UnsubscribeHeadersFootersEvents;
begin
  Headers.UnsubscribeEvents;
  Footers.UnsubscribeEvents;
end;

function TdxSection.Copy(CopyManager: TObject): TdxSection;
var
  ATargetModel: TdxDocumentModel;
  ATargetPosition: TdxDocumentModelPosition;
  ASectionIndex: TdxSectionIndex;
  AResultSection: TdxSection;
  ALastParagraph: TdxParagraph;
  ACopyManager: TdxDocumentModelCopyManager absolute CopyManager;
begin
  Assert(DocumentModel = ACopyManager.SourceModel);
  ATargetModel := ACopyManager.TargetModel;
  ATargetPosition := ACopyManager.TargetPosition;
  ATargetModel.InsertSection(ATargetPosition.LogPosition);
  ASectionIndex := ATargetModel.MainPieceTable.LookupSectionIndexByParagraphIndex(ATargetPosition.ParagraphIndex);
  AResultSection := ATargetModel.Sections[ASectionIndex];
  AResultSection.CopyFromCore(Self);
  ALastParagraph := ATargetModel.MainPieceTable.Paragraphs[AResultSection.LastParagraphIndex];
  TdxDocumentModel(DocumentModel).MainPieceTable.Paragraphs[LastParagraphIndex].CopyFrom(ATargetModel, ALastParagraph);

  AResultSection.Headers.CopyFrom(Self);
  AResultSection.Footers.CopyFrom(Self);

  Result := AResultSection;
end;

function TdxSection.GetActualColumnsCount: Integer;
begin
  if Columns.EqualWidthColumns then
    Result := Columns.ColumnCount
  else
    Result := Columns.Info.Columns.Count;
end;

function TdxSection.IsHidden: Boolean;
var
  AParagraphs: TdxParagraphCollection;
  I: TdxParagraphIndex;
begin
  AParagraphs := TdxDocumentModel(DocumentModel).MainPieceTable.Paragraphs;
  for I := FirstParagraphIndex to LastParagraphIndex do
    if not AParagraphs[I].IsHidden then
      Exit(False);
  Result := True;
end;

function TdxSection.GetFirstPageHeader: TdxSectionHeader;
begin
  if GeneralSettings.DifferentFirstPage then
    Result := InnerFirstPageHeader
  else
    Result := nil;
end;

function TdxSection.GetInnerFirstPageHeader: TdxSectionHeader;
begin
  Result := Headers.GetObject(TdxHeaderFooterType.First);
end;

function TdxSection.GetInnerFirstPageHeaderIndex: TdxHeaderIndex;
begin
  Result := Headers.GetObjectIndex(TdxHeaderFooterType.First);
end;

procedure TdxSection.SetInnerFirstPageHeaderIndex(const AValue: TdxHeaderIndex);
begin
  Headers.SetObjectIndex(TdxHeaderFooterType.First, AValue);
end;

function TdxSection.GetInnerOddPageHeader: TdxSectionHeader;
begin
  Result := Headers.GetObject(TdxHeaderFooterType.Odd);
end;

function TdxSection.GetInnerOddPageHeaderIndex: TdxHeaderIndex;
begin
  Result := Headers.GetObjectIndex(TdxHeaderFooterType.Odd);
end;

procedure TdxSection.SetInnerOddPageHeaderIndex(const AValue: TdxHeaderIndex);
begin
  Headers.SetObjectIndex(TdxHeaderFooterType.Odd, AValue);
end;

function TdxSection.GetEvenPageHeader: TdxSectionHeader;
begin
  if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
    Result := InnerEvenPageHeader
  else
    Result := InnerOddPageHeader;
end;

function TdxSection.GetInnerEvenPageHeader: TdxSectionHeader;
begin
  Result := Headers.GetObject(TdxHeaderFooterType.Even);
end;

function TdxSection.GetInnerEvenPageHeaderIndex: TdxHeaderIndex;
begin
  Result := Headers.GetObjectIndex(TdxHeaderFooterType.Even);
end;

procedure TdxSection.SetInnerEvenPageHeaderIndex(const AValue: TdxHeaderIndex);
begin
  Headers.SetObjectIndex(TdxHeaderFooterType.Even, AValue);
end;

function TdxSection.GetFirstPageFooter: TdxSectionFooter;
begin
  if GeneralSettings.DifferentFirstPage then
    Result := InnerFirstPageFooter
  else
    Result := nil;
end;

function TdxSection.GetInnerFirstPageFooter: TdxSectionFooter;
begin
  Result := Footers.GetObject(TdxHeaderFooterType.First);
end;

function TdxSection.GetInnerFirstPageFooterIndex: TdxFooterIndex;
begin
  Result := Footers.GetObjectIndex(TdxHeaderFooterType.First);
end;

procedure TdxSection.SetInnerFirstPageFooterIndex(const AValue: TdxFooterIndex);
begin
  Footers.SetObjectIndex(TdxHeaderFooterType.First, AValue);
end;

function TdxSection.GetInnerOddPageFooter: TdxSectionFooter;
begin
  Result := Footers.GetObject(TdxHeaderFooterType.Odd);
end;

function TdxSection.GetInnerOddPageFooterIndex: TdxFooterIndex;
begin
  Result := Footers.GetObjectIndex(TdxHeaderFooterType.Odd);
end;

procedure TdxSection.SetInnerOddPageFooterIndex(const AValue: TdxFooterIndex);
begin
  Footers.SetObjectIndex(TdxHeaderFooterType.Odd, AValue);
end;

function TdxSection.GetEvenPageFooter: TdxSectionFooter;
begin
  if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
    Result := InnerEvenPageFooter
  else
    Result := InnerOddPageFooter;
end;

function TdxSection.GetInnerEvenPageFooter: TdxSectionFooter;
begin
  Result := Footers.GetObject(TdxHeaderFooterType.Even);
end;

function TdxSection.GetInnerEvenPageFooterIndex: TdxFooterIndex;
begin
  Result := Footers.GetObjectIndex(TdxHeaderFooterType.Even);
end;

procedure TdxSection.SetInnerEvenPageFooterIndex(const AValue: TdxFooterIndex);
begin
  Footers.SetObjectIndex(TdxHeaderFooterType.Even, AValue);
end;

procedure TdxSection.UnsubscribeInnerObjectsEvents;
begin
  FMargins.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FColumns.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FPage.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FGeneralSettings.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FPageNumbering.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FLineNumbering.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FFootNote.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
  FEndNote.OnObtainAffectedRange.Remove(OnObtainAffectedRange);
end;

procedure TdxSection.Reset;
var
  ACache: TdxDocumentCache;
begin
  ACache := TdxDocumentModel(DocumentModel).Cache;
  Page.CopyFrom(ACache.PageInfoCache[0]);
  Margins.CopyFrom(ACache.MarginsInfoCache[0]);
  PageNumbering.CopyFrom(ACache.PageNumberingInfoCache[0]);
  GeneralSettings.CopyFrom(ACache.GeneralSectionInfoCache[0]);
  LineNumbering.CopyFrom(ACache.LineNumberingInfoCache[0]);
  Columns.CopyFrom(ACache.ColumnsInfoCache[0]);
  FootNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex]);
  EndNote.CopyFrom(ACache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex]);
end;

function TdxSection.GetPreviousSection: TdxSection;
var
  AIndex: TdxSectionIndex;
begin
  AIndex := TdxDocumentModel(DocumentModel).Sections.IndexOf(Self);
  if AIndex <= 0 then
    Result := nil
  else
    Result := TdxDocumentModel(DocumentModel).Sections[AIndex - 1];
end;

function TdxSection.GetNextSection: TdxSection;
var
  AIndex: TdxSectionIndex;
begin
  AIndex := TdxDocumentModel(DocumentModel).Sections.IndexOf(Self);
  if AIndex + 1 >= TdxDocumentModel(DocumentModel).Sections.Count then
    Result := nil
  else
    Result := TdxDocumentModel(DocumentModel).Sections[AIndex + 1];
end;

function TdxSection.GetCorrespondingHeader(AFooter: TdxSectionFooter): TdxSectionHeader;
begin
  Result := Headers.GetObject(AFooter.&Type);
end;

function TdxSection.GetCorrespondingFooter(AHeader: TdxSectionHeader): TdxSectionFooter;
begin
  Result := Footers.GetObject(AHeader.&Type);
end;

function TdxSection.GetPreviousSectionHeader(AHeader: TdxSectionFooter): TdxSectionHeader;
var
  APreviousSection: TdxSection;
begin
  APreviousSection := GetPreviousSection;
  if APreviousSection = nil then
    Result := nil
  else
    Result := APreviousSection.Headers.GetObject(AHeader.&Type);
end;

function TdxSection.GetPreviousSectionFooter(AFooter: TdxSectionFooter): TdxSectionFooter;
var
  APreviousSection: TdxSection;
begin
  APreviousSection := GetPreviousSection;
  if APreviousSection = nil then
    Result := nil
  else
    Result := APreviousSection.Footers.GetObject(AFooter.&Type);
end;

{ TdxSectionCollection }

function TdxSectionCollection.First: TdxSection;
begin
  Result := TdxSection(inherited First);
end;

function TdxSectionCollection.Last: TdxSection;
begin
  Result := TdxSection(inherited Last);
end;

function TdxSectionCollection.GetItem(Index: Integer): TdxSection;
begin
  Result := TdxSection(inherited Items[Index]);
end;

procedure TdxSectionCollection.SetItem(Index: Integer; const Value: TdxSection);
begin
  inherited Items[Index] := Value;
end;

{ TdxSectionEventArgs }

constructor TdxSectionEventArgs.Create(ASectionIndex: TdxSectionIndex);
begin
  inherited Create;
  FSectionIndex := ASectionIndex;
end;

{ TdxSectionProperties }

constructor TdxSectionProperties.Create(ASection: TdxSection);
begin
  inherited Create;
  FSection := ASection;
  FPageWidth := ASection.Page.Width;
  FPageHeight := ASection.Page.Height;
  FLeftMargin := ASection.Margins.Left;
  FRightMargin := ASection.Margins.Right;
  TopMargin := ASection.Margins.Top;
  BottomMargin := ASection.Margins.Bottom;
  FColumnInfoCollection := ASection.Columns.GetColumns;
  FEqualWidthColumns := ASection.Columns.EqualWidthColumns;
  FColumnCount := ASection.Columns.ColumnCount;
  FSpace := ASection.Columns.Space;
end;

constructor TdxSectionProperties.Create(ASectionProperties: TdxSectionProperties);
begin
  inherited Create;
  FSection := ASectionProperties.Section;
  FPageWidth := ASectionProperties.FPageWidth;
  FPageHeight := ASectionProperties.FPageHeight;
  FLeftMargin := ASectionProperties.FLeftMargin;
  FRightMargin := ASectionProperties.FRightMargin;
  TopMargin := ASectionProperties.TopMargin;
  BottomMargin := ASectionProperties.BottomMargin;
  FColumnInfoCollection := Section.Columns.GetColumns;
  FEqualWidthColumns := ASectionProperties.FEqualWidthColumns;
  FColumnCount := Section.Columns.ColumnCount;
  FSpace := ASectionProperties.FSpace;
end;

destructor TdxSectionProperties.Destroy;
begin
  FColumnInfoCollection.Free;
  inherited Destroy;
end;

procedure TdxSectionProperties.CopyToSection(ASection: TdxSection);
begin
  ASection.Margins.Left := LeftMargin;
  ASection.Margins.Right := RightMargin;
  ASection.Margins.Top := TopMargin;
  ASection.Margins.Bottom := BottomMargin;
  ASection.Columns.SetColumns(ColumnInfoCollection);
  ASection.Columns.ColumnCount := ColumnCount;
  ASection.Columns.EqualWidthColumns := EqualWidthColumns;
  ASection.Columns.Space := Space;
  ASection.Page.Width := PageWidth;
  ASection.Page.Height := PageHeight;
end;

{ TdxSectionHeaderFooterBase }

constructor TdxSectionHeaderFooterBase.Create(ADocumentModel: TdxCustomDocumentModel; AType: TdxHeaderFooterType);
begin
  inherited Create(ADocumentModel);
  FType := AType;
end;

function TdxSectionHeaderFooterBase.GetIsMain: Boolean;
begin
  Result := False;
end;


function TdxSectionHeaderFooterBase.GetIsHeaderFooter: Boolean;
begin
  Result := True;
end;

function TdxSectionHeaderFooterBase.RaiseRequestSectionIndex: TdxSectionIndex;
var
  AArgs: TdxRequestSectionIndexEventArgs;
begin
  if not FOnRequestSectionIndex.Empty then
  begin
    AArgs := TdxRequestSectionIndexEventArgs.Create;
    try
      FOnRequestSectionIndex.Invoke(Self, AArgs);
      if AArgs.SectionIndex = MaxInt then
        AArgs.SectionIndex := -1;
      Result := AArgs.SectionIndex;
    finally
      AArgs.Free;
    end;
  end
  else
    Result := -1;
end;

function TdxSectionHeaderFooterBase.GetSectionIndex: TdxSectionIndex;
var
  AIndex: TdxSectionIndex;
begin
  if DocumentModel.IsUpdateLocked and TdxDocumentModel(DocumentModel).DeferredChanges.IsSetContentMode then
    Exit(0);

  AIndex := RaiseRequestSectionIndex;
  if AIndex < 0 then
    Result := 0
  else
    Result := AIndex;
end;

function TdxSectionHeaderFooterBase.GetSection: TdxSection;
begin
  Result := TdxDocumentModel(DocumentModel).Sections[GetSectionIndex];
end;

function TdxSectionHeaderFooterBase.CalculateMainPieceTableStartRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
var
  AActualSection: TdxSection;
  APieceTable: TdxPieceTable;
begin
  if ARunIndex = dxRunIndexDontCare then
    Exit(ARunIndex);

  AActualSection := GetSection;
  while GetContainer(AActualSection).IsLinkedToPrevious(&Type) do
    AActualSection := AActualSection.GetPreviousSection;

  APieceTable := TdxDocumentModel(DocumentModel).MainPieceTable;
  Result := APieceTable.Paragraphs[AActualSection.FirstParagraphIndex].FirstRunIndex;
end;

function TdxSectionHeaderFooterBase.CalculateMainPieceTableEndRunIndex(ARunIndex: TdxRunIndex): TdxRunIndex;
var
  AActualSection, ANextSection: TdxSection;
  APieceTable: TdxPieceTable;
begin
  if ARunIndex = dxRunIndexDontCare then
    Exit(ARunIndex);

  AActualSection := GetSection;
  while True do
  begin
    ANextSection := AActualSection.GetNextSection;
    if ANextSection = nil then
      Break;

    if GetContainer(ANextSection).IsLinkedToPrevious(&Type) then
      AActualSection := ANextSection
    else
      Break;
  end;

  APieceTable := TdxDocumentModel(DocumentModel).MainPieceTable;
  Result := APieceTable.Paragraphs[AActualSection.LastParagraphIndex].LastRunIndex;
end;

procedure TdxSectionHeaderFooterBase.OverrideType(AType: TdxHeaderFooterType);
begin
  FType := AType;
end;

function TdxSectionHeaderFooterBase.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
begin
  Result := inherited LookupSectionIndexByParagraphIndex(GetSection.FirstParagraphIndex);
end;

procedure TdxSectionHeaderFooterBase.ApplyChanges(AChangeType: TdxDocumentModelChangeType; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  inherited ApplyChanges(AChangeType, CalculateMainPieceTableStartRunIndex(AStartRunIndex), CalculateMainPieceTableEndRunIndex(AEndRunIndex));
end;

procedure TdxSectionHeaderFooterBase.ApplyChangesCore(AActions: TdxDocumentModelChangeActions; AStartRunIndex: TdxRunIndex; AEndRunIndex: TdxRunIndex);
begin
  if TdxDocumentModelChangeAction.SplitRunByCharset in AActions then
  begin
    inherited ApplyChangesCore([TdxDocumentModelChangeAction.SplitRunByCharset], AStartRunIndex, AEndRunIndex);
    Exclude(AActions, TdxDocumentModelChangeAction.SplitRunByCharset);
  end;
  inherited ApplyChangesCore(AActions, CalculateMainPieceTableStartRunIndex(AStartRunIndex), CalculateMainPieceTableEndRunIndex(AEndRunIndex));
end;

procedure TdxSectionHeaderFooterBase.ApplySectionFormatting(ALogPositionStart: TdxDocumentLogPosition; ALength: Integer; AModifier: TdxSectionPropertyModifierBase);
begin
  DocumentModel.ApplySectionFormatting(TdxDocumentModel(DocumentModel).MainPieceTable.Paragraphs[GetSection.FirstParagraphIndex].LogPosition, 1, AModifier);
end;

procedure TdxSectionHeaderFooterBase.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
begin
end;

{ TdxSectionHeadersFootersBase }

constructor TdxSectionHeadersFootersBase.Create(ASection: TdxSection);
begin
  inherited Create;
  Assert(ASection <> nil);
  FSection := ASection;
end;

function TdxSectionHeadersFootersBase.GetDocumentModel: TdxCustomDocumentModel;
begin
  Result := FSection.DocumentModel;
end;

function TdxSectionHeadersFootersBase.CalculateActualObjectCore(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TdxSectionHeaderFooterBase;
begin
  if AFirstPageOfSection then
  begin
    if Section.GeneralSettings.DifferentFirstPage then
      Exit(GetObjectCore(TdxHeaderFooterType.First));
  end;

  if AIsEvenPage then
  begin
    if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
      Exit(GetObjectCore(TdxHeaderFooterType.Even))
    else
      Exit(GetObjectCore(TdxHeaderFooterType.Odd));
  end
  else
    Exit(GetObjectCore(TdxHeaderFooterType.Odd));
end;

function TdxSectionHeadersFootersBase.CalculateActualObjectType(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TdxHeaderFooterType;
var
  AResult: TdxHeaderFooterType;
begin
  AResult := TdxHeaderFooterType.Odd;
  if AFirstPageOfSection then
  begin
    if Section.GeneralSettings.DifferentFirstPage then
      AResult := TdxHeaderFooterType.First;
  end;

  if AResult <> TdxHeaderFooterType.Odd then
    Exit(AResult);

  if AIsEvenPage then
  begin
    if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
      Exit(TdxHeaderFooterType.Even)
    else
      Exit(TdxHeaderFooterType.Odd);
  end
  else
    Exit(TdxHeaderFooterType.Odd);
end;

{ TdxSectionHeaderFooterIndexChangedHistoryItem }

constructor TdxSectionHeaderFooterIndexChangedHistoryItem<T>.Create(ASection: TdxSection);
begin
  inherited Create(GetMainPieceTable(ASection));
  FSectionIndex := TdxDocumentModel(DocumentModel).Sections.IndexOf(ASection);
end;

class function TdxSectionHeaderFooterIndexChangedHistoryItem<T>.GetMainPieceTable(ASection: TdxSection): TdxCustomPieceTable;
begin
  Assert(ASection <> nil);
  Result := ASection.DocumentModel.MainPart;
end;

procedure TdxSectionHeaderFooterIndexChangedHistoryItem<T>.RedoCore;
begin
  ReplaceHeaderFooter(NextIndex);
end;

procedure TdxSectionHeaderFooterIndexChangedHistoryItem<T>.UndoCore;
begin
  ReplaceHeaderFooter(PreviousIndex);
end;

procedure TdxSectionHeaderFooterIndexChangedHistoryItem<T>.ReplaceHeaderFooter(AIndex: T);
var
  ASection: TdxSection;
begin
  ASection := TdxDocumentModel(DocumentModel).Sections[FSectionIndex];
  SetCurrentHeaderFooterIndex(ASection, AIndex);
  ApplyChanges(ASection);
end;

procedure TdxSectionHeaderFooterIndexChangedHistoryItem<T>.ApplyChanges(ASection: TdxSection);
var
  AArgs: TdxObtainAffectedRangeEventArgs;
begin
  AArgs := TdxObtainAffectedRangeEventArgs.Create;
  try
    ASection.OnObtainAffectedRange(Self, AArgs);
    if AArgs.Start >= 0 then
      TdxPieceTable(PieceTable).ApplyChangesCore(
        TdxSectionMarginsChangeActionsCalculator.CalculateChangeActions(TdxSectionMarginsChangeType.Top),
        AArgs.Start, AArgs.&End);
  finally
    AArgs.Free;
  end;
end;

{ TdxSectionPageHeaderIndexChangedHistoryItem }

procedure TdxSectionPageHeaderIndexChangedHistoryItem.SetCurrentHeaderFooterIndex(ASection: TdxSection; AIndex: TdxHeaderIndex);
begin
  ASection.Headers.SetObjectIndex(&Type, AIndex);
end;

{ TdxSectionHeadersFooters<TObj> }

constructor TdxSectionHeadersFooters<TObj>.Create(ASection: TdxSection);
var
  I: Integer;
begin
  inherited Create(ASection);
  SetLength(FIndices, 3);
  for I := 0 to 3 - 1 do
    FIndices[I] := InvalidIndex;
end;

procedure TdxSectionHeadersFooters<TObj>.CopyFrom(ASource: TdxSectionHeadersFooters<TObj>);
var
  I: Integer;
begin
  Assert(DocumentModel = ASource.DocumentModel);
  UnsubscribeEvents;
  try
    for I := 0 to 3 - 1 do
      FIndices[I] := ASource.FIndices[I];
  finally
    SubscribeEvents;
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.Add(AType: TdxHeaderFooterType);
begin
  ChangeObjectIndex(AType, CreateEmptyObject(AType));
end;

procedure TdxSectionHeadersFooters<TObj>.Remove(AType: TdxHeaderFooterType);
begin
  ChangeObjectIndex(AType, InvalidIndex);
end;

function TdxSectionHeadersFooters<TObj>.CanLinkToPrevious(AType: TdxHeaderFooterType): Boolean;
begin
  Result := Section.GetPreviousSection <> nil;
end;

function TdxSectionHeadersFooters<TObj>.IsLinkedToPrevious(AType: TdxHeaderFooterType): Boolean;
var
  APreviousSection: TdxSection;
begin
  APreviousSection := Section.GetPreviousSection;
  if APreviousSection = nil then
    Exit(False);
  Result := GetObjectProvider(APreviousSection).GetObjectIndex(AType) = GetObjectIndex(AType);
end;

function TdxSectionHeadersFooters<TObj>.IsLinkedToNext(AType: TdxHeaderFooterType): Boolean;
var
  ANextSection: TdxSection;
begin
  ANextSection := Section.GetNextSection;
  if ANextSection = nil then
    Exit(False);
  Result := GetObjectProvider(ANextSection).GetObjectIndex(AType) = GetObjectIndex(AType);
end;

procedure TdxSectionHeadersFooters<TObj>.LinkToPrevious(AType: TdxHeaderFooterType);
begin
  PerformLinkToPrevious(AType, True, LinkCore);
end;

procedure TdxSectionHeadersFooters<TObj>.UnlinkFromPrevious(AType: TdxHeaderFooterType);
begin
  PerformLinkToPrevious(AType, False, UnlinkCore);
end;

procedure TdxSectionHeadersFooters<TObj>.LinkToNext(AType: TdxHeaderFooterType);
begin
  PerformLinkToNext(AType, True, LinkCore);
end;

procedure TdxSectionHeadersFooters<TObj>.UnlinkFromNext(AType: TdxHeaderFooterType);
begin
  PerformLinkToNext(AType, False, UnlinkCore);
end;

procedure TdxSectionHeadersFooters<TObj>.SubscribeEvents;
var
  AObjectIndices: TdxIntegerList;
  ACount, I: Integer;
begin
  AObjectIndices := GetValidUniqueObjectIndices;
  try
    ACount := AObjectIndices.Count;
    if ACount <= 0 then
      Exit;

    for I := 0 to ACount - 1 do
      SubscribeObjectEvents(ObjectCache[AObjectIndices[I]]);
  finally
    AObjectIndices.Free;
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.UnsubscribeEvents;
var
  AObjectIndices: TdxIntegerList;
  I, ACount: Integer;
begin
  AObjectIndices := GetValidUniqueObjectIndices;
  try
    ACount := AObjectIndices.Count;
    if ACount <= 0 then
      Exit;

    for I := 0 to ACount - 1 do
      UnsubscribeObjectEvents(ObjectCache[AObjectIndices[I]]);
  finally
    AObjectIndices.Free;
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.SubscribeObjectEvents(AObj: TObj);
begin
  AObj.RequestSectionIndex.Add(OnRequestSectionIndex);
end;

procedure TdxSectionHeadersFooters<TObj>.UnsubscribeObjectEvents(AObj: TObj);
begin
  AObj.RequestSectionIndex.Remove(OnRequestSectionIndex);
end;

procedure TdxSectionHeadersFooters<TObj>.OnRequestSectionIndex(ASender: TObject; AArgs: TdxRequestSectionIndexEventArgs);
var
  AIndex: TdxSectionIndex;
begin
  AIndex := TdxDocumentModel(DocumentModel).Sections.IndexOf(Section);
  if AIndex < 0 then
    TdxRichEditExceptions.ThrowInternalException;
  AArgs.SectionIndex := Min(AIndex, AArgs.SectionIndex);
end;

function TdxSectionHeadersFooters<TObj>.GetValidUniqueObjectIndices: TdxIntegerList;
var
  I: Integer;
  AIndex: TdxSectionIndex;
begin
  Result := TdxIntegerList.Create;
  for I := 0 to 3 - 1 do
  begin
    AIndex := FIndices[I];
    if (AIndex <> InvalidIndex) and not Result.Contains(AIndex) then
      Result.Add(AIndex);
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.PerformLinkToPrevious(AType: TdxHeaderFooterType; ALink: Boolean;
  const ALinkAction: TdxLinkObjectDelegate);
var
  APreviousSection: TdxSection;
  APreviousSectionObjectIndex: TdxSectionIndex;
  ARelinkNextSection: Boolean;
begin
  APreviousSection := Section.GetPreviousSection;
  if APreviousSection = nil then
    Exit;

  APreviousSectionObjectIndex := GetObjectProvider(APreviousSection).GetObjectIndex(AType);
  if (APreviousSectionObjectIndex = GetObjectIndex(AType)) = ALink then
    Exit;

  ARelinkNextSection := ShouldRelinkNextSection(AType);

  ALinkAction(Self, AType, APreviousSectionObjectIndex);

  if ARelinkNextSection then
    GetObjectProvider(Section.GetNextSection).PerformLinkToPrevious(AType, True, LinkCore);
end;

function TdxSectionHeadersFooters<TObj>.ShouldRelinkNextSection(AType: TdxHeaderFooterType): Boolean;
var
  ANextSection: TdxSection;
begin
  ANextSection := Section.GetNextSection;
  if ANextSection <> nil then
    Result := GetObjectProvider(ANextSection).GetObjectIndex(AType) = GetObjectIndex(AType)
  else
    Result := False;
end;

procedure TdxSectionHeadersFooters<TObj>.PerformLinkToNext(AType: TdxHeaderFooterType; ALink: Boolean;
  const ALinkAction: TdxLinkObjectDelegate);
var
  ANextSection: TdxSection;
  ANextSectionObjectIndex: TdxSectionIndex;
  ARelinkPrevSection: Boolean;
begin
  ANextSection := Section.GetNextSection;
  if ANextSection = nil then
    Exit;

  ANextSectionObjectIndex := GetObjectProvider(ANextSection).GetObjectIndex(AType);
  if (ANextSectionObjectIndex = GetObjectIndex(AType)) = ALink then
    Exit;

  ARelinkPrevSection := ShouldRelinkPreviousSection(AType);

  ALinkAction(Self, AType, ANextSectionObjectIndex);

  if ARelinkPrevSection then
    GetObjectProvider(Section.GetPreviousSection).PerformLinkToNext(AType, True, LinkCore);
end;

function TdxSectionHeadersFooters<TObj>.ShouldRelinkPreviousSection(AType: TdxHeaderFooterType): Boolean;
var
  APrevSection: TdxSection;
begin
  APrevSection := Section.GetPreviousSection;
  if APrevSection <> nil then
    Result := GetObjectProvider(APrevSection).GetObjectIndex(AType) = GetObjectIndex(AType)
  else
    Result := False;
end;

class procedure TdxSectionHeadersFooters<TObj>.LinkCore(ATargetObject: TdxSectionHeadersFooters<TObj>;
  AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex);
begin
  ATargetObject.ChangeObjectIndex(AType, ASectionObjectIndex);
end;

class procedure TdxSectionHeadersFooters<TObj>.UnlinkCore(ATargetObject: TdxSectionHeadersFooters<TObj>;
  AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex);
begin
  ATargetObject.UnlinkCore(AType, ASectionObjectIndex);
end;

procedure TdxSectionHeadersFooters<TObj>.UnlinkCore(AType: TdxHeaderFooterType; ASectionObjectIndex: TdxSectionIndex);
var
  ASectionObject: TObj;
begin
  if ASectionObjectIndex = InvalidIndex then
    Add(AType)
  else
  begin
    ASectionObject := ObjectCache[ASectionObjectIndex];
    ChangeObjectIndex(AType, CreateObjectDeepCopy(ASectionObject));
  end;
end;

function TdxSectionHeadersFooters<TObj>.GetObjectIndex(AType: TdxHeaderFooterType): TdxSectionIndex;
begin
  Result := FIndices[Integer(AType)];
end;

procedure TdxSectionHeadersFooters<TObj>.SetObjectIndex(AType: TdxHeaderFooterType; AIndex: TdxSectionIndex);
begin
  UnsubscribeEvents;
  try
    FIndices[Integer(AType)] := AIndex;
  finally
    SubscribeEvents;
  end;
end;

function TdxSectionHeadersFooters<TObj>.GetObject(AType: TdxHeaderFooterType): TObj;
var
  AIndex: TdxSectionIndex;
begin
  AIndex := GetObjectIndex(AType);
  if AIndex = InvalidIndex then
    Result := nil
  else
    Result := ObjectCache[AIndex];
end;

function TdxSectionHeadersFooters<TObj>.GetObjectCore(AType: TdxHeaderFooterType): TdxSectionHeaderFooterBase;
begin
  Result := GetObject(AType);
end;

function TdxSectionHeadersFooters<TObj>.CalculateActualObject(AFirstPageOfSection: Boolean; AIsEvenPage: Boolean): TObj;
begin
  Result := TObj(CalculateActualObjectCore(AFirstPageOfSection, AIsEvenPage));
end;

function TdxSectionHeadersFooters<TObj>.CreateEmptyObject(AType: TdxHeaderFooterType): TdxSectionIndex;
var
  ANewObject: TObj;
begin
  ANewObject := CreateEmptyObjectCore(AType);
  TdxDocumentModel(DocumentModel).UnsafeEditor.InsertFirstParagraph(TdxPieceTable(ANewObject.PieceTable));
  Result := CreateIndex(ObjectCache.Count);
  ObjectCache.Add(ANewObject);
end;

function TdxSectionHeadersFooters<TObj>.CreateObjectDeepCopy(ASourceObject: TObj): TdxSectionIndex;
begin
  Result := CreateEmptyObject(ASourceObject.&Type);
  CopyObjectContent(ASourceObject, ObjectCache[Result]);
end;

procedure TdxSectionHeadersFooters<TObj>.CopyObjectContent(ASourceHeaderFooter: TObj; ATargetHeaderFooter: TObj);
var
  ASource, ATarget: TdxPieceTable;
  ACopyManager: TdxDocumentModelCopyManager;
  AOperation: TdxCopySectionOperation;
begin
  ASource := TdxPieceTable(ASourceHeaderFooter.PieceTable);
  ATarget := TdxPieceTable(ATargetHeaderFooter.PieceTable);
  ACopyManager := TdxDocumentModelCopyManager.Create(ASource, ATarget, TdxParagraphNumerationCopyOptions.CopyAlways);
  try
    AOperation := TdxCopySectionOperation(ASource.DocumentModel.CreateCopySectionOperation(ACopyManager));
    try
      AOperation.FixLastParagraph := True;
      AOperation.Execute(ASource.DocumentStartLogPosition, ASource.DocumentEndLogPosition - ASource.DocumentStartLogPosition + 1, False);
    finally
      AOperation.Free;
    end;
  finally
    ACopyManager.Free;
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.CopyFrom(ASourceSection: TdxSection);
var
  ASource: TdxSectionHeadersFooters<TObj>;
  I: Integer;
  AType: TdxHeaderFooterType;
  ASourceObject: TObj;
begin
  ASource := GetObjectProvider(ASourceSection);
  for I := 0 to 3 - 1 do
  begin
    AType := TdxHeaderFooterType(I);
    if not ASource.IsLinkedToPrevious(AType) then
    begin
      ASourceObject := ASource.GetObject(AType);
      if ASourceObject <> nil then
      begin
        Add(AType);
        CopyObjectContent(ASourceObject, GetObject(AType));
      end;
    end
    else
      LinkToPrevious(AType);
  end;
end;

procedure TdxSectionHeadersFooters<TObj>.ChangeObjectIndex(AType: TdxHeaderFooterType; ANewIndex: TdxSectionIndex);
var
  AItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxSectionIndex>;
begin
  DocumentModel.BeginUpdate;
  try
    AItem := CreateHistoryItem;
    AItem.&Type := AType;
    AItem.PreviousIndex := GetObjectIndex(AItem.&Type);
    AItem.NextIndex := ANewIndex;

    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
end;

{ TdxHeaderFooterCollectionBase }

procedure TdxHeaderFooterCollectionBase<T, U>.Clear;
var
  I, ACount: Integer;
begin
  ACount := Count;
  for I := 0 to ACount - 1 do
    Items[I].PieceTable.Clear;
  inherited Clear;
end;

{ TdxSectionHeader }

function TdxSectionHeader.GetIsHeader: Boolean;
begin
  Result := True;
end;

function TdxSectionHeader.GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Headers;
end;

function TdxSectionHeader.GetCaption: string;
begin
  if &Type = TdxHeaderFooterType.First then
    Exit(cxGetResourceString(@sdxRichEditCaption_FirstPageHeader));

  if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
  begin
    if &Type = TdxHeaderFooterType.Odd then
      Exit(cxGetResourceString(@sdxRichEditCaption_OddPageHeader))
    else
      Exit(cxGetResourceString(@sdxRichEditCaption_EvenPageHeader));
  end
  else
    Exit(cxGetResourceString(@sdxRichEditCaption_PageHeader));
end;

{ TdxSectionHeaders }

function TdxSectionHeaders.GetInvalidIndex: TdxHeaderIndex;
begin
  Result := dxHeaderIndexInvalid;
end;

function TdxSectionHeaders.GetObjectCache: TdxHeaderFooterCollectionBase<TdxSectionHeader, TdxHeaderIndex>;
begin
  Result := TdxDocumentModel(DocumentModel).Headers;
end;

function TdxSectionHeaders.CreateEmptyObjectCore(AType: TdxHeaderFooterType): TdxSectionHeader;
begin
  Result := TdxSectionHeader.Create(DocumentModel, AType);
end;

function TdxSectionHeaders.CreateIndex(AValue: Integer): TdxHeaderIndex;
begin
  Result := AValue;
end;

function TdxSectionHeaders.CreateHistoryItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxHeaderIndex>;
begin
  Result := TdxSectionPageHeaderIndexChangedHistoryItem.Create(Section);
end;

function TdxSectionHeaders.GetObjectProvider(ASection: TdxSection): TdxSectionHeadersFooters<TdxSectionHeader>;
begin
  Result := ASection.Headers;
end;

{ TdxSectionFooter }

function TdxSectionFooter.GetIsFooter: Boolean;
begin
  Result := True;
end;

function TdxSectionFooter.GetContainer(ASection: TdxSection): TdxSectionHeadersFootersBase;
begin
  Result := ASection.Footers;
end;

function TdxSectionFooter.GetCaption: string;
begin
  if &Type = TdxHeaderFooterType.First then
    Exit(cxGetResourceString(@sdxRichEditCaption_FirstPageFooter));

  if TdxDocumentModel(DocumentModel).DocumentProperties.DifferentOddAndEvenPages then
  begin
    if &Type = TdxHeaderFooterType.Odd then
      Result := cxGetResourceString(@sdxRichEditCaption_OddPageFooter)
    else
      Result := cxGetResourceString(@sdxRichEditCaption_EvenPageFooter)
  end
  else
    Result := cxGetResourceString(@sdxRichEditCaption_PageFooter);
end;

{ TdxSectionPageFooterIndexChangedHistoryItem }

procedure TdxSectionPageFooterIndexChangedHistoryItem.SetCurrentHeaderFooterIndex(ASection: TdxSection; AIndex: TdxFooterIndex);
begin
  ASection.Footers.SetObjectIndex(&Type, AIndex);
end;

{ TdxSectionFooters }

function TdxSectionFooters.GetInvalidIndex: TdxFooterIndex;
begin
  Result := dxFooterIndexInvalid;
end;

function TdxSectionFooters.GetObjectCache: TdxHeaderFooterCollectionBase<TdxSectionFooter, TdxFooterIndex>;
begin
  Result := TdxDocumentModel(DocumentModel).Footers;
end;

function TdxSectionFooters.CreateEmptyObjectCore(AType: TdxHeaderFooterType): TdxSectionFooter;
begin
  Result := TdxSectionFooter.Create(DocumentModel, AType);
end;

function TdxSectionFooters.CreateIndex(AValue: Integer): TdxFooterIndex;
begin
  Result := AValue;
end;

function TdxSectionFooters.CreateHistoryItem: TdxSectionHeaderFooterIndexChangedHistoryItem<TdxFooterIndex>;
begin
  Result := TdxSectionPageFooterIndexChangedHistoryItem.Create(Section);
end;

function TdxSectionFooters.GetObjectProvider(ASection: TdxSection): TdxSectionHeadersFooters<TdxSectionFooter>;
begin
  Result := ASection.Footers;
end;

end.
